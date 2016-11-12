angular.module('news', ['ngMaterial', 'ngSanitize'])
    .config(function ($provide, $httpProvider) {
	$provide.factory('HttpInterceptor', function($q, $rootScope) {
	    $rootScope.loading = false;
	    return {
		request: function(config) {
		    $rootScope.loading = true;
		    return config || $q.when(config);
		},
		requestError: function(rejection) {
		    $rootScope.loading = false;
		    return $q.reject(rejection);
		},
		response:  function (response) {
		    $rootScope.loading = false;
		    return response || $q.when(response);
		},
		responseError:  function (rejection) {
		    $rootScope.loading = false;
		    return $q.reject(rejection);
		}
	    };
	});
	$httpProvider.interceptors.push('HttpInterceptor');
    })

    .controller('providerCtrl', function($scope, $http, $mdDialog, $rootScope, $sce) {
	$scope.providers = [];
	$scope.summaries = [];
	$scope.urls = {};
	$scope.languages = {};
	$scope.hide_provider = {};
	$scope.offsets = {};
	$scope.filters = {};

	$http.get("/news/providers").
	    then(function (response) {
		$scope.providers = response.data;
		$scope.providers.forEach(function (provider, index) {
		    $scope.urls[provider.name] = provider.url;
		    provider.languages.forEach(function (lang) {
			$scope.languages[lang] = false;
		    });
		});
		$scope.load_summaries();
	    });
	
	$scope.is_loading = function() {
	    return $rootScope.loading;
	};
	$scope.load_summaries = function () {
	    var names = $scope.providers.map(function(p) { return p.name; });
	    $http.post("/news/summary", { providers: names }).
		then(function (response) {
		    $scope.summaries = response.data;			
		    $scope.summaries.forEach(function(summary) {
			$scope.offsets[summary.provider] = summary.feeds.length;
		    });
		    // FIXME magic number
		    var len = Math.floor($scope.summaries.length / 3);
		    var index = (len) * 3;
		    while (len-- > 0) {
			var dummy = {provider: false, feeds: [], dummy: Math.random()};
			$scope.summaries.splice(index, 0, dummy);
			index -= 3;
		    }
		});
	};
	$scope.show_link = function (ev, link, title) {
	    $mdDialog.show({
		templateUrl: '/news/html/iframe.html',
		parent: angular.element(document.body),
		targetEvent: ev,
		clickOutsideToClose: true,
		controller: 'iFrameCtrl',
		fullscreen: true,
		resolve: {
		    url: function () {
			return link;
		    },
		    title: function() {
			return title;
		    }
		}
	    });
	};
	$scope.load_summary = function(provider) {
	    // only for UX, it's better to see something is working...
	    $scope.summaries.forEach(function(summary){
		if (summary.provider === provider) {
		    summary.feeds = [];
		}
	    });
	    var callback = function (response) {
		var feeds = response.data;
		$scope.summaries.forEach(function(summary){
		    if (summary.provider === provider) {
			summary.feeds = feeds;
		    }
		});
		$scope.offsets[provider] = feeds.length;
	    };
	    if ($scope.filters[provider]) {
		$http.post("/news/summary/" + provider,
			   { feed_url: $scope.filters[provider] })
		    .then(callback);
	    } else {
		$http.get("/news/summary/" + provider).then(callback);
	    }
	};
	$scope.filter_by_url = function(provider, url) {
	    $scope.filters[provider] = url;
	    $scope.load_summary(provider);
	};
	$scope.clear_filter = function(provider) {
	    $scope.filters[provider] = false;
	    $scope.load_summary(provider);
	};
	$scope.read_more = function(provider, offset) {
	    var callback = function (response) {
		if (response.data.length != 0) {
		    $scope.summaries[provider] = $scope.summaries[provider].concat(response.data);
		    $scope.offsets[provider] += response.data.length;
		} else {
		    $scope.offsets[provider] = false;
		}
	    };
	    var url = "/news/summary/" + provider;
	    if ($scope.filters[provider]) {
		$http.post(url, { "offset": offset, feed_url: $scope.filters[provider] })
		    .then(callback);
	    } else {
		$http.get(url + "?offset=" + offset).then(callback);
	    }
	};
	$scope.hide_by_lang = function(lang) {
	    $scope.providers.forEach(function(provider) {
		if (lang === '*') {
		    $scope.hide_provider[provider.name] = false;
		} else {
		    $scope.hide_provider[provider.name] = (provider.languages.indexOf(lang) < 0);
		}
	    });
	    for (var key in $scope.languages) {
		if (lang === '*') {
		    $scope.languages[key] = false;
		} else {
		    $scope.languages[key] = !(key === lang);
		}
	    }
	};
	$scope.is_multi_lingual = function() {
	    var size = 0;
	    for (var key in $scope.languages) {
		size++;
	    }
	    return size > 1;
	    
	};
	$scope.insert_thing = function(index) {
	    return $sce.trustAsHtml(retrieve_insertion($http, index));
	};
    })

    .controller('iFrameCtrl', function($scope, $mdDialog, $sce, url, title) {
	$scope.givenUrl = $sce.trustAsResourceUrl(url);
	$scope.title = title;
	$scope.close = function () {
	    $mdDialog.cancel();
	}
    })

    .directive('insertThing', function($http) {
	return function(scope, element, attr) {
	    if (!scope.provider) {
		angular.element(element).removeClass("feed-contents");
		angular.element(element).addClass("inserted");
	    }
	};
    });

function check_frame(iframe) {
//    var doc = iframe.contentWindow;
//    console.log(doc);
//    if (doc.innerText == "") {
//	var parent = iframe.parentNode;
//	parent.removeChild(iframe);
//	parent.innerHTML = '<div>Failed to load: click the title to see on new wndow</div>';
//    }
}