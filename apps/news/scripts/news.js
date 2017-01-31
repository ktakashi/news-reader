// From http://stackoverflow.com/questions/11381673/detecting-a-mobile-browser
function check_mobile() {
    var check = false;
    (function(a){if(/(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows ce|xda|xiino/i.test(a)||/1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-/i.test(a.substr(0,4))) check = true;})(navigator.userAgent||navigator.vendor||window.opera);
    return check;
}

function check_device_size() {
    return window.matchMedia('(max-device-width: 799px)').matches;
}

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

    .controller('providerCtrl', function($scope, $http, $mdDialog, $rootScope) {
	$scope.providers = [];
	$scope.summaries = [];
	$scope.urls = {};
	$scope.current_language = "*";
	$scope.languages = {};
	$scope.hide_provider = {};
	$scope.offsets = {};
	$scope.filters = {};
	$scope.is_mobile = check_device_size();
	$scope.shown = {};
	$scope.selected_feed = {};
	$scope.query = { max_date: new Date() };
	
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
	    clear_filters($scope);
	    request_summaries($http, create_request_parameter($scope));
	};
	if ($scope.is_mobile) {
	    $scope.show_link = function (ev, link, title) {
		window.open(link, '_blank');
	    };
	} else {
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
	};
	$scope.load_summary = function($event, provider) {
	    var target = $event.currentTarget || $event.target;
	    var icon = angular.element(target).children().children();
	    icon.addClass('round');
	    load_summary(provider, function() {
		icon.removeClass('round');
	    });
	};
	$scope.filter_by_url = function(provider, url) {
	    $scope.filters[provider] = url;
	    load_summary(provider, function() {});
	};
	$scope.filter_feed = function(provider) {
	    $scope.filter_by_url(provider, $scope.selected_feed[provider].url);
	};
	$scope.clear_filter = function(provider) {
	    $scope.filters[provider] = false;
	    $scope.selected_feed[provider] = false;
	    load_summary(provider, function() {});
	};
	$scope.read_more = function(provider, offset) {
	    var callback = function (response) {
		if (response.data.length != 0) {
		    var feeds = response.data;
		    $scope.summaries.forEach(function(summary){
			if (summary.provider === provider) {
			    summary.feeds = summary.feeds.concat(feeds);
			}
		    });
		    $scope.offsets[provider] += feeds.length;
		} else {
		    $scope.offsets[provider] = false;
		}
	    };
	    var param = create_request_parameter($scope);
	    delete param['providers'];
	    if ($scope.filters[provider]) {
		param.feeds = [ $scope.filters[provider] ];
	    }
	    param.offset = offset;
	    request_summary(provider, param, callback);
	};
	$scope.hide_by_lang = function(lang) {
	    $scope.current_language = lang;
	    $scope.providers.forEach(function(provider) {
		if (lang === '*') {
		    $scope.hide_provider[provider.name] = false;
		} else {
		    $scope.hide_provider[provider.name] =
			(provider.languages.indexOf(lang) < 0);
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
	$scope.tweet = function(s) {
	    var link = "https://twitter.com/intent/tweet?text="
		+ get_title(s.title) + "&url=" + encode(s.link);
	    window.open(link, '_blank');
	};
	$scope.find_feeds = function(provider_name) {
	    for (var i = 0; i < $scope.providers.length; i++) {
		var p = $scope.providers[i];
		if (p.name === provider_name) return p.feeds;
	    }
	    return [];
	};

	$scope.search_feeds = function() {
	    $scope.query.toggle = false;
	    request_summaries($http, create_request_parameter($scope));
	};

	function request_summaries($http, param) {
	    $http.post("/news/summary", param)
		.then(function (response) {
		    $scope.summaries = response.data;			
		    $scope.summaries.forEach(function(summary) {
			$scope.offsets[summary.provider] = summary.feeds.length;
		    });
		});
	}
	function create_request_parameter($scope) {
	    var param = { limit: 10 };
	    if ($scope.query.provider) {
		param.providers = [$scope.query.provider.name];
		if ($scope.query.feeds) {
		    param.feeds = $scope.query.feeds;
		}
	    } else {
		param.providers = $scope.providers.map(function(p) {
		    return p.name;
		});
	    }
	    return populate_time_request_paramater($scope, param);
	}
	function populate_time_request_paramater($scope, param) {
	    if ($scope.query.from) {
		param.from = $scope.query.from.getTime() /1000;
	    }
	    if ($scope.query.to) {
		param.to = $scope.query.to.getTime() /1000;
	    }
	    return param;
	}
	function clear_filters($scope) {
	    $scope.filters = {};
	    $scope.selected_feed = {};
	    $scope.query = {};
	}
	function load_summary(provider, thunk) {
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
		thunk();
	    };
	    var param = populate_time_request_paramater($scope, {});
	    if ($scope.filters[provider]) {
		param.feeds = [ $scope.filters[provider] ];
	    } else if ($scope.query.feeds) {
		param.feeds = $scope.query.feeds;
	    }
	    request_summary(provider, param, callback);
	}
	function request_summary(provider, param, callback) {
	    if (!'limit' in param) param.limit = 10;
	    if (!'offset' in param) param.offset = 0;
	    
	    if (is_object_empty(param, "limit", "offset")) {
		$http.get("/news/summary/" + provider + "?offset=" + param.offset + "&limit=" + param.limit).then(callback);
	    } else {
		$http.post("/news/summary/" + provider, param).then(callback);
	    }
	}
	function get_title (title) {
	    return encode("\"" + title + "\" via news-reader.nl");
	}
	function encode(str) {
	    return encodeURIComponent(str);
	}
	function is_object_empty(obj) {
	    var rest = Array.from(arguments).slice(1);
	    for (var k in obj) {
		if (rest.indexOf(k) != -1) continue;
		return false;
	    }
	    return true;
	}

    })

    .controller('iFrameCtrl', function($scope, $mdDialog, $sce, url, title) {
	$scope.givenUrl = $sce.trustAsResourceUrl(url);
	$scope.title = title;
	$scope.close = function () {
	    $mdDialog.cancel();
	};
    })

// Based on https://github.com/EricWVGG/AngularSlideables
// License of slidable
/*
The MIT License (MIT)

Copyright (c) 2013 Eric Jacobsen

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
    .directive('slideToggle', function() {
	return {
	    restrict: 'A',
	    link: function(scope, element, attrs) {
		if (scope.is_mobile) {
		    var target, svg;
		    attrs.expanded = false;
		    element.bind('click', function() {
			if (!target) {
			    var tmp = document.getElementById(attrs.slideToggle);
			    target = angular.element(tmp);
			}
			// FIXME this is depending on AngularJS 
			if (!svg) {
			    var tmp = document.getElementById(attrs.slideToggle + " toggle");
			    svg = angular.element(tmp);
			}
			
			if(!attrs.expanded) {
			    target.addClass('show');
			    svg.addClass('arrow-up');
			} else {
			    target.removeClass('show');
			    svg.removeClass('arrow-up');
			}
			attrs.expanded = !attrs.expanded;
			scope.shown[attrs.slideToggle] = attrs.expanded;
			scope.$apply();
		    });
		}
	    }
	};
    })
;

function check_frame(iframe) {
//    var doc = iframe.contentWindow;
//    console.log(doc);
//    if (doc.innerText == "") {
//	var parent = iframe.parentNode;
//	parent.removeChild(iframe);
//	parent.innerHTML = '<div>Failed to load: click the title to see on new wndow</div>';
//    }
}
